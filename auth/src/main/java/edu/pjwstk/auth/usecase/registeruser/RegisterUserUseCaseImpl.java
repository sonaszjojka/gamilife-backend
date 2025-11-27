package edu.pjwstk.auth.usecase.registeruser;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.dto.RegisterUserApiDto;
import edu.pjwstk.auth.validators.PasswordValidator;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;

@Service
@Validated
@AllArgsConstructor
public class RegisterUserUseCaseImpl implements RegisterUserUseCase {
    private final PasswordEncoder passwordEncoder;
    private final PasswordValidator passwordValidator;
    private final UserApi userApi;

    @Override
    @Transactional
    public BasicUserInfoApiDto executeInternal(RegisterUserCommand cmd) {
        passwordValidator.validate(cmd.password());

        RegisterUserApiDto user = new RegisterUserApiDto(
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

        return userApi.registerNewUser(user);
    }
}
