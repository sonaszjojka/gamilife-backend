package pl.gamilife.auth.usecase.changepassword;

import pl.gamilife.auth.exception.domain.InvalidCredentialsException;
import pl.gamilife.auth.exception.domain.OldAndNewPasswordAreTheSameException;
import pl.gamilife.auth.validators.PasswordValidator;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

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
