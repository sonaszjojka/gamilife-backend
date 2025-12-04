package pl.gamilife.user.usecase.impl;

import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.gamification.GamificationApi;
import pl.gamilife.api.gamification.dto.StartingGamificationValuesDto;
import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.api.user.dto.RegisterUserApiDto;
import pl.gamilife.infrastructure.core.exception.common.domain.UserAlreadyExistsException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetUserByEmailUseCase;
import pl.gamilife.user.usecase.RegisterNewUserUseCase;

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

        StartingGamificationValuesDto startingGamificationValues = gamificationApi.getStartingGamificationValues();
        User newUser = new User(
                UUID.randomUUID(),
                dto.firstName(),
                dto.lastName(),
                dto.email(),
                dto.password(),
                dto.username(),
                dto.dateOfBirth(),
                startingGamificationValues.level(),
                startingGamificationValues.experience(),
                startingGamificationValues.money(),
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
