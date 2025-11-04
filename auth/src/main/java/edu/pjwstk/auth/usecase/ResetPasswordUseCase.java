package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.command.ResetPasswordCommand;
import jakarta.validation.Valid;

public interface ResetPasswordUseCase {
    void execute(@Valid ResetPasswordCommand command);
}
