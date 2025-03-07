
class UsersController < ApplicationController
  def index
    users = User.all
    render json: users
  end

  def show
    user = User.find(params[:id])
    if user
      render json: user
    else
      render json: { error: 'User not found' }, status: :not_found
    end
  end

  def create
    user = User.new(user_params)
    if user.save
      render json: user, status: :created
    else
      render json: { error: 'User creation failed' }, status: :unprocessable_entity
    end
  end

  def update
    user = User.find(params[:id])
    if user
      if user.update(user_params)
        render json: user, status: :ok
      else
        render json: { error: 'User update failed' }, status: :unprocessable_entity
      end
    else
      render json: { error: 'User not found' }, status: :not_found
    end
  end

  def destroy
    user = User.find(params[:id])
    if user
      user.destroy
      render json: { message: 'User deleted successfully' }, status: :ok
    else
      render json: { error: 'User not found' }, status: :not_found
    end
  end

  private

  def user_params
    params.require(:user).permit(:name, :email)
  end
end
