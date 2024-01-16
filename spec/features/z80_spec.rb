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
    sleep 40
    # check that Elm is still running
    expect(page).to have_content 'Refresh Interval'
  end
end