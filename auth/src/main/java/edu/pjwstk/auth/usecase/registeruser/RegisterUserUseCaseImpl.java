package edu.pjwstk.auth.usecase.registeruser;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.dto.RegisterUserApiDto;
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
    private final UserApi userApi;

    @Override
    @Transactional
    public BasicUserInfoApiDto execute(RegisterUserCommand registerUserCommand) {
        RegisterUserApiDto user = new RegisterUserApiDto(
                registerUserCommand.firstName(),
                registerUserCommand.lastName(),
                registerUserCommand.email(),
                passwordEncoder.encode(registerUserCommand.password()),
                registerUserCommand.username(),
                registerUserCommand.dateOfBirth(),
                registerUserCommand.sendBudgetReports(),
                registerUserCommand.isProfilePublic(),
                false
        );

        return userApi.registerNewUser(user);
    }
}
