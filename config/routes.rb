Rails.application.routes.draw do
  root 'games#index'

  resources :games, only: [:index, :show]
  # resources :images, only: :get
  get 'images/:data/:foreground/:background' => 'images#show', :as=>:image
end
