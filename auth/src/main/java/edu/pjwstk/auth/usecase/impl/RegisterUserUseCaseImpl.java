package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.dto.service.RegisterUserDto;
import edu.pjwstk.auth.usecase.RegisterUserUseCase;
import edu.pjwstk.auth.util.PasswordValidator;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.security.InvalidParameterException;

@Service
@AllArgsConstructor
public class RegisterUserUseCaseImpl implements RegisterUserUseCase {

    private final PasswordValidator passwordValidator;
    private final PasswordEncoder passwordEncoder;
    private final UserApi userApi;

    @Override
    @Transactional
    public BasicUserInfoApiDto execute(RegisterUserDto registerUserDto) {
        if (!passwordValidator.validate(registerUserDto.password())) {
            throw new InvalidParameterException("Password must be at least 8 characters long, contain at least one letter, one digit, and one special character.");
        }

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
