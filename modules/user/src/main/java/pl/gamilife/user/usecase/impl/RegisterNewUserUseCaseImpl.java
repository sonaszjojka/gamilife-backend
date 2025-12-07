package pl.gamilife.user.usecase.impl;

import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.api.user.dto.RegisterUserApiDto;
import pl.gamilife.infrastructure.core.event.UserRegisteredEvent;
import pl.gamilife.infrastructure.core.exception.domain.UserAlreadyExistsException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetUserByEmailUseCase;
import pl.gamilife.user.usecase.RegisterNewUserUseCase;

import java.time.Instant;
import java.util.UUID;

@Service
@AllArgsConstructor
public class RegisterNewUserUseCaseImpl implements RegisterNewUserUseCase {

    private final UserRepository userRepository;
    private final GetUserByEmailUseCase getUserByEmailUseCase;
    private final ApplicationEventPublisher eventPublisher;

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
                0,
                0,
                0,
                dto.sendBudgetReports(),
                dto.isProfilePublic(),
                dto.isEmailVerified(),
                dto.isTutorialCompleted(),
                Instant.now()
        );
        userRepository.save(newUser);

        eventPublisher.publishEvent(new UserRegisteredEvent(newUser.getId()));

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
