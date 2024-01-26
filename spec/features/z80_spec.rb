require "rails_helper"

RSpec.describe "Spectrum Emulator" do
  before do
    Game.create! :name => 'Match Day', :tapfile => 'MATCHDAY.tap'
  end

  let(:expected_hz) { (ENV['HZ'] || "8.6").to_f }

  it "loads the emulator", :js do
    visit '/'
    click_on 'Match Day'
    # check that Elm is running
    expect(page).to have_content 'Refresh Interval'
    # Test emulation speed in Hz
    low = 0
    high = page.find("#hz").text.to_f
    # wait for speed to hit a steady state
    while (high - low).abs > 0.01
      times = 1.upto(6).map do
        sleep 0.15
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