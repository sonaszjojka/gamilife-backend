package edu.pjwstk.auth.usecase.changepassword;

import jakarta.validation.Valid;

public interface ChangePasswordUseCase {
    String execute(@Valid ChangePasswordInternalCommand dto);
}
