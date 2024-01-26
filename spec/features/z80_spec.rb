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
    old = 0
    new = page.find("#hz").text.to_f
    # wait for speed to hit a steady state
    while (new - old).abs > 0.01
      sleep 1.5
      old = new
      new = page.find("#hz").text.to_f
    end
    expect(new).to be > 9.4
    p "Speed #{new} Hz"
  end
end