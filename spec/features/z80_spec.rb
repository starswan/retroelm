require "rails_helper"

RSpec.describe "Spectrum Emulator" do
  before do
    Game.create! :name => 'Match Day', :tapfile => 'MATCHDAY.tap'
  end

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
      sleep 0.5
      f1 = page.find("#hz").text.to_f
      sleep 0.5
      f2 = page.find("#hz").text.to_f
      sleep 0.5
      f3 = page.find("#hz").text.to_f
      sorted = [f1, f2, f3].sort
      low = sorted.first
      high = sorted.last
    end
    expect(high).to be > 9.4
    p "Speed #{high} Hz"
  end
end