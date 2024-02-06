require "rails_helper"

RSpec.describe "Spectrum Emulator" do
  before do
    create :game, :match_day
  end

  let(:expected_hz) { (ENV['HZ'] || "9.0").to_f }

  it "loads the emulator", :js do
    visit '/'
    click_on 'Match Day'
    # check that Elm is running
    expect(page).to have_content 'Refresh Interval'
    sleep 1
    # Test emulation speed in Hz
    low = 0
    high = page.find("#hz").text.to_f
    # wait for speed to hit a steady state
    while (high - low).abs > 0.01
      times = 1.upto(6).map do
        sleep 0.2
        page.find("#hz").text.to_f
      end
      sorted = times.sort
      low = sorted.first
      high = sorted.last
    end
    expect(high).to be > expected_hz
    p "Speed #{high} Hz"
  end
end