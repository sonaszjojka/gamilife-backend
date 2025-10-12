package edu.pjwstk.auth.usecase;

public interface LogoutUserUseCase {
    void execute(String refreshToken);
}
