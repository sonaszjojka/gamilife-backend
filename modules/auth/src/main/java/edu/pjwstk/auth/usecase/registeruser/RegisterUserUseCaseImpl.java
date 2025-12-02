package edu.pjwstk.auth.usecase.registeruser;

import pl.gamification.api.user.UserApi;
import pl.gamification.api.user.dto.BasicUserInfoApiDto;
import pl.gamification.api.user.dto.RegisterUserApiDto;
import edu.pjwstk.auth.validators.PasswordValidator;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional
@AllArgsConstructor
public class RegisterUserUseCaseImpl implements RegisterUserUseCase {
    private final PasswordEncoder passwordEncoder;
    private final PasswordValidator passwordValidator;
    private final UserApi userApi;

    @Override
    public BasicUserInfoApiDto execute(RegisterUserCommand cmd) {
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
