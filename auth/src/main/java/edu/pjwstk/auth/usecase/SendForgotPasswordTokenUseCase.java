package edu.pjwstk.auth.usecase;

public interface SendForgotPasswordTokenUseCase {
    void execute(String email);
}
