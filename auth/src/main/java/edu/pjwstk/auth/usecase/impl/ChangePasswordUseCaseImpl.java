package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.exceptions.InvalidCredentialsException;
import edu.pjwstk.auth.exceptions.OldAndNewPasswordAreTheSameException;
import edu.pjwstk.auth.usecase.ChangePasswordUseCase;
import edu.pjwstk.auth.usecase.command.ChangePasswordInternalCommand;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

@Service
@Validated
@AllArgsConstructor
public class ChangePasswordUseCaseImpl implements ChangePasswordUseCase {

    private PasswordEncoder passwordEncoder;

    @Override
    public String execute(ChangePasswordInternalCommand dto) {
        if (!passwordEncoder.matches(dto.providedPassword(), dto.hashedUserPassword())) {
            throw new InvalidCredentialsException("Invalid password");
        }

        if (passwordEncoder.matches(dto.newPassword(), dto.hashedUserPassword())) {
            throw new OldAndNewPasswordAreTheSameException();
        }

        return passwordEncoder.encode(dto.newPassword());
    }
}
