package edu.pjwstk.auth.usecase.sendforgotpasswordcode;

public interface SendForgotPasswordTokenUseCase {
    void execute(String email);
}
