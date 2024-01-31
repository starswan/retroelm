require 'rails_helper'

RSpec.describe ImagesController, type: :controller do
  describe "get" do
    context "all white" do
      it "gets an image" do
        get :show, params: {data: 65535, foreground: 0, background: 0 }
        expect(response.body).to eq('xxx')
      end
    end
  end
end
