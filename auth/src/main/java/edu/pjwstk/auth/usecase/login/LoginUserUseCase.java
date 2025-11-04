package edu.pjwstk.auth.usecase.login;

public interface LoginUserUseCase {
    LoginUserResult execute(LoginUserCommand loginUserCommand);
}
