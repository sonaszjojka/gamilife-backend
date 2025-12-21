package pl.gamilife.auth.application.registeruser;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.auth.domain.model.projection.RegisterUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.validator.PasswordValidator;

@Service
@Transactional
@AllArgsConstructor
public class RegisterUserUseCaseImpl implements RegisterUserUseCase {
    private final PasswordEncoder passwordEncoder;
    private final PasswordValidator passwordValidator;
    private final UserContext userContext;

    @Override
    public BasicUserDetails execute(RegisterUserCommand cmd) {
        passwordValidator.validate(cmd.password());

        RegisterUserDetails user = new RegisterUserDetails(
                cmd.firstName(),
                cmd.lastName(),
                cmd.email(),
                passwordEncoder.encode(cmd.password()),
                cmd.username(),
                cmd.dateOfBirth(),
                cmd.sendBudgetReports(),
                cmd.isProfilePublic(),
                false,
                false
        );

        return userContext.registerNewUser(user);
    }
}
