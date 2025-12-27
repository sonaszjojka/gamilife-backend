package pl.gamilife.user.usecase.impl;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.api.user.dto.RegisterUserDto;
import pl.gamilife.shared.kernel.event.UserRegisteredEvent;
import pl.gamilife.shared.kernel.exception.domain.UserAlreadyExistsException;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetUserByEmailUseCase;
import pl.gamilife.user.usecase.RegisterNewUserUseCase;

@Service
@RequiredArgsConstructor
public class RegisterNewUserUseCaseImpl implements RegisterNewUserUseCase {

    private final UserRepository userRepository;
    private final GetUserByEmailUseCase getUserByEmailUseCase;
    private final ApplicationEventPublisher eventPublisher;

    @Value("${app.timezone.default}")
    private String defaultTimezone;

    @Override
    @Transactional
    public BasicUserInfoDto execute(RegisterUserDto dto) {
        if (getUserByEmailUseCase.execute(dto.email()).isPresent()) {
            throw new UserAlreadyExistsException("This email address is already taken");
        }

        User newUser = User.create(
                dto.firstName(),
                dto.lastName(),
                dto.email(),
                dto.password(),
                dto.username(),
                dto.dateOfBirth(),
                dto.sendBudgetReports(),
                dto.isProfilePublic(),
                dto.isEmailVerified(),
                dto.timezone() == null ? defaultTimezone : dto.timezone().getId()
        );

        userRepository.save(newUser);

        eventPublisher.publishEvent(new UserRegisteredEvent(newUser.getId()));

        return new BasicUserInfoDto(
                newUser.getId(),
                newUser.getEmail(),
                newUser.getUsername(),
                newUser.getLevel(),
                newUser.getExperience(),
                newUser.getMoney()
        );
    }
}
