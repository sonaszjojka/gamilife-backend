package edu.pjwstk.auth.usecase.logout;

public interface LogoutUserUseCase {
    void execute(String refreshToken);
}
