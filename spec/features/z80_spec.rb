require "rails_helper"

# require "open-uri"

RSpec.describe "Spectrum Emulator" do
  before do
    z80_game.save!
  end

  let(:expected_hz) { (ENV['HZ'] || "11.21").to_f }

  # disable for now, as we don't want to run the test twice really
  # xcontext "with match day" do
  #   let(:z80_game) { build(:game, :match_day) }
  #
  #   it "loads the emulator", :js do
  #     visit '/'
  #     click_on z80_game.name
  #
  #     expect(page).to have_content 'Refresh Interval'
  #     speed = measure_speed_in_hz
  #     p "Speed #{speed} Hz"
  #     expect(speed).to be >= expected_hz
  #   end
  # end

  context "with z80_full_test" do
    let(:z80_game) { build(:game, :z80_test_full) }
    let(:version) { "1.2a" }
    let(:z80_test_url) { "https://github.com/raxoft/z80test/releases/download/v#{version}/z80test-#{version}.zip" }
    let(:faraday) {
      Faraday.new do |f|
        f.response :raise_error
        f.response :follow_redirects
        f.adapter Faraday.default_adapter
      end
    }
    let(:z80full_directory) { Rails.root.join("public", "z80test") }

    before do
      unless File.exist? z80full_directory
        load_tapfile z80_test_url, z80full_directory
      end

      visit '/'
      click_on z80_game.name
    end

    it "loads the emulator", :js do
      # check that Elm is running
      expect(page).to have_content 'Refresh Interval'
      speed = measure_speed_in_hz
      expect(speed).to be > expected_hz
      p "Speed #{speed} Hz"
    end
  end

  def measure_speed_in_hz
    sleep 10
    # Test emulation speed in Hz
    low = 0
    high = page.find("#hz").text.to_f
    # wait for speed to hit a steady state
    while high - low > 0.02
      times = 1.upto(8).map do
        sleep 0.4
        page.find("#hz").text.to_f
      end
      # p "Speed Times #{times.sort}"
      low = times.min
      high = times.max
    end
    high
  end

  def load_tapfile input_url, output_directory
    zip_data = []
    faraday.get(input_url) do |req|
      req.options.on_data = Proc.new do |chunk, size|
        zip_data  << chunk
      end
    end
    zipdata = zip_data.join
    zipfile = StringIO.new zipdata
    Dir.mkdir output_directory
    Zip::InputStream.open(zipfile) do |zip_stream|
      while (entry = zip_stream.get_next_entry)
        entry_filename = entry.name.split("/").last
        if entry.name.ends_with?(".tap")
          data = entry.get_input_stream.read
          File.open("#{output_directory}/#{entry_filename}", "wb+") do |file|
            file.write(data)
          end
        end
      end
    end
  end
end