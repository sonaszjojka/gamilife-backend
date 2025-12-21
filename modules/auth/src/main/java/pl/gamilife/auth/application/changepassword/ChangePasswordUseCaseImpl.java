package pl.gamilife.auth.application.changepassword;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import pl.gamilife.auth.domain.exception.domain.InvalidCredentialsException;
import pl.gamilife.auth.domain.exception.domain.OldAndNewPasswordAreTheSameException;
import pl.gamilife.auth.domain.validator.PasswordValidator;

@Service
@AllArgsConstructor
public class ChangePasswordUseCaseImpl implements ChangePasswordUseCase {

    private final PasswordValidator passwordValidator;
    private final PasswordEncoder passwordEncoder;

    @Override
    public String execute(ChangePasswordCommand cmd) {
        passwordValidator.validate(cmd.newPassword());

        if (!passwordEncoder.matches(cmd.providedPassword(), cmd.hashedUserPassword())) {
            throw new InvalidCredentialsException("Invalid password");
        }

        if (passwordEncoder.matches(cmd.newPassword(), cmd.hashedUserPassword())) {
            throw new OldAndNewPasswordAreTheSameException();
        }

        return passwordEncoder.encode(cmd.newPassword());
    }
}
