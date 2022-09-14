require 'rails_helper'

RSpec.describe GamesController, type: :controller do
  fixtures :games

  it "should get index" do
    get :index
    assert_response :success
    expect( assigns(:games)).not_to be_nil
  end

  it "should get new" do
    get :new
    assert_response :success
  end

  it "should create game" do
    expect do
      post :create, params: { :game => { name: 'game', tapfile: 'help.tap' } }
    end.to change(Game, :count).by(1)

    assert_redirected_to game_path(assigns(:game))
  end

  it "should show game" do
    get :show, params: { :id => games(:one).to_param }
    assert_response :success
  end

  it "should get edit" do
    get :edit, params: { :id => games(:one).to_param }
    assert_response :success
  end

  it "should update game" do
    put :update, params: { :id => games(:one).to_param, :game => { tapfile: 'help.tap' } }
    assert_redirected_to game_path(assigns(:game))
  end

  it "should destroy game" do
    expect do
      delete :destroy, params: { :id => games(:one).to_param }
    end.to change(Game, :count).by(-1)

    assert_redirected_to games_path
  end
end
