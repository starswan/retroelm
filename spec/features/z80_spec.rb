require "rails_helper"

RSpec.describe "Spectrum Emulator" do
  before do
    Game.create! :name => 'Match Day', :tapfile => 'MATCHDAY.tap'
  end

  it "loads the emulator", :js do
    visit '/'
    sleep 5
    click_on 'Match Day'
    # Let the code run to initialization point
    if ENV.key? 'CI'
      sleep 10
    else
      sleep 30
    end
    # check that Elm is running
    expect(page).to have_content 'Refresh Interval'
    # Test emulation speed in Hz
    expect(page.find("#hz").text.to_f).to be > 9.3
  end
end