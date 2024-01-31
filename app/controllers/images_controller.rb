class ImagesController < ApplicationController
  def show
    # data is 0..65535
    @data = params[:data]
    # foreground is 0..7
    @foreground = params[:foreground]
    # background is 0..7
    @background = params[:background]
    @image = MiniMagick::Image.new do |image|
      image.draw_point 0, 0
    end
  end
end
