package edu.pjwstk.auth.usecase;

public interface GenerateAndSendForgotPasswordTokenUseCase {
    void execute(String email);
}
