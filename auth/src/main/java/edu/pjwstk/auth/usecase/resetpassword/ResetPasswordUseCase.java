package edu.pjwstk.auth.usecase.resetpassword;

import jakarta.validation.Valid;

public interface ResetPasswordUseCase {
    void execute(@Valid ResetPasswordCommand command);
}
