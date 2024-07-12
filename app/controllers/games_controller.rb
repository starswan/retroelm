class GamesController < ApplicationController
  # GET /games
  # GET /games.xml
  def index
    @games = Game.all

    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @games }
    end
  end

  # GET /games/1
  # GET /games/1.xml
  def show
    @game = Game.find(params[:id])
    @homepage = "#{request.protocol}#{request.host_with_port}/"

    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @game }
      format.jnlp
    end
  end
end
