package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.dto.service.RegisterUserDto;
import edu.pjwstk.auth.usecase.RegisterUserUseCase;
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
    public BasicUserInfoApiDto execute(RegisterUserDto registerUserDto) {
        RegisterUserApiDto user = new RegisterUserApiDto(
                registerUserDto.firstName(),
                registerUserDto.lastName(),
                registerUserDto.email(),
                passwordEncoder.encode(registerUserDto.password()),
                registerUserDto.username(),
                registerUserDto.dateOfBirth(),
                registerUserDto.sendBudgetReports(),
                registerUserDto.isProfilePublic(),
                false
        );

        return userApi.registerNewUser(user);
    }
}
