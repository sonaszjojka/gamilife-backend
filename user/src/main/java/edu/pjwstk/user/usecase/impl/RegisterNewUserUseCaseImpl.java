package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.api.gamification.GamificationApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.dto.RegisterUserApiDto;
import edu.pjwstk.core.exception.common.domain.UserAlreadyExistsException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.GetUserByEmailUseCase;
import edu.pjwstk.user.usecase.RegisterNewUserUseCase;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.UUID;

@Service
@AllArgsConstructor
public class RegisterNewUserUseCaseImpl implements RegisterNewUserUseCase {

    private final GamificationApi gamificationApi;
    private final UserRepository userRepository;
    private final GetUserByEmailUseCase getUserByEmailUseCase;

    @Override
    @Transactional
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
                1,
                0,
                0,
                dto.sendBudgetReports(),
                dto.isProfilePublic(),
                dto.isEmailVerified(),
                dto.isTutorialCompleted(),
                Instant.now()
        );
        userRepository.save(newUser);

        gamificationApi.initUserStatisticsFor(newUser.getId());

        return new BasicUserInfoApiDto(
                newUser.getId(),
                newUser.getEmail(),
                newUser.getUsername(),
                newUser.getLevel(),
                newUser.getExperience(),
                newUser.getMoney()
        );
    }
}
