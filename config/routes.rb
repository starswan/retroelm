Rails.application.routes.draw do
  root 'games#index'

  resources :games, only: [:index, :show]
  # resources :images, only: :get
  get 'images/:data' => 'images#show', :as=>:image
end
