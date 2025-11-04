package edu.pjwstk.auth.usecase;

import edu.pjwstk.auth.usecase.command.ChangePasswordInternalCommand;
import jakarta.validation.Valid;

public interface ChangePasswordUseCase {
    String execute(@Valid ChangePasswordInternalCommand dto);
}
