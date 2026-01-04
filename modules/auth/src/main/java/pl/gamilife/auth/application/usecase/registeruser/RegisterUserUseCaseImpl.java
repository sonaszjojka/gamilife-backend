package pl.gamilife.auth.application.usecase.registeruser;

import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.auth.application.dto.AuthTokens;
import pl.gamilife.auth.application.service.TokenService;
import pl.gamilife.auth.domain.model.projection.BasicUserDetails;
import pl.gamilife.auth.domain.model.projection.RegisterUserDetails;
import pl.gamilife.auth.domain.port.context.UserContext;
import pl.gamilife.auth.domain.service.EmailVerificationService;
import pl.gamilife.auth.domain.validator.PasswordValidator;

@Service
@Transactional
@AllArgsConstructor
public class RegisterUserUseCaseImpl implements RegisterUserUseCase {

    private final PasswordEncoder passwordEncoder;
    private final PasswordValidator passwordValidator;
    private final UserContext userContext;
    private final EmailVerificationService emailVerificationService;
    private final TokenService tokenService;

    @Override
    public AuthTokens execute(RegisterUserCommand cmd) {
        passwordValidator.validate(cmd.password());

        RegisterUserDetails userToRegister = new RegisterUserDetails(
                cmd.firstName(),
                cmd.lastName(),
                cmd.email(),
                passwordEncoder.encode(cmd.password()),
                cmd.username(),
                cmd.dateOfBirth(),
                cmd.sendBudgetReports(),
                cmd.isProfilePublic(),
                false,
                false,
                cmd.zoneId()
        );

        BasicUserDetails user = userContext.registerNewUser(userToRegister);

        String code = emailVerificationService.generateAndSaveEmailVerificationCode(user.userId());
        emailVerificationService.sendEmailVerificationCode(user.userId(), code);

        return tokenService.generateTokenPair(
                user.userId(),
                user.email(),
                userToRegister.isEmailVerified()
        );
    }
}
