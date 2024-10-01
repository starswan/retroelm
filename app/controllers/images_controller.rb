class ImagesController < ApplicationController
  def show
    # data is 0..65535 - so 8x2 pixels (4 needed for a screen square)
    # could start with 0..255 for simplicity (maybe hex string?)
    @data = params[:data]
    # we can do fg / bg using CSS
    # foreground is 0..7
    # @foreground = params[:foreground]
    # background is 0..7
    # @background = params[:background]
    @image = MiniMagick::Image.new do |image|
      image.draw_point 0, 0
    end
  end
end
