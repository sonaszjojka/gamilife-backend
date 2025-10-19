package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;
import edu.pjwstk.common.userApi.exception.UserAlreadyExistsException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.GetUserByEmailUseCase;
import edu.pjwstk.user.usecase.RegisterNewUserUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class RegisterNewUserUseCaseImpl implements RegisterNewUserUseCase {

    private final UserRepository userRepository;
    private final GetUserByEmailUseCase getUserByEmailUseCase;

    @Override
    public BasicUserInfoApiDto execute(RegisterUserApiDto dto) {
        if (getUserByEmailUseCase.execute(dto.email()).isPresent()) {
            throw new UserAlreadyExistsException("This email address is already taken");
        }

        User newUser = new User(
                UUID.randomUUID(),
                dto.firstName(),
                dto.lastName(),
                dto.email(),
                dto.password(),
                dto.username(),
                dto.dateOfBirth(),
                0,
                0,
                dto.sendBudgetReports(),
                dto.isProfilePublic(),
                dto.isEmailVerified()
        );

        userRepository.save(newUser);

        return new BasicUserInfoApiDto(
                newUser.getId(),
                newUser.getEmail()
        );
    }
}
