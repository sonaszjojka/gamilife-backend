package pl.gamilife.user.usecase.updateusertimezone;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.event.UserTimezoneChangedEvent;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.UserRepository;

@Service
@Transactional
@AllArgsConstructor
public class UpdateUserTimezoneUseCaseImpl implements UpdateUserTimezoneUseCase {

    private final UserRepository userRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public Void execute(UpdateUserTimezoneCommand cmd) {
        User user = userRepository.getUserById(cmd.userId()).orElseThrow(
                () -> new UserNotFoundException("User not found")
        );

        String currentTimezone = user.getTimezone();
        boolean timezoneChanged = user.tryUpdateTimezone(cmd.timezone());
        userRepository.save(user);

        if (timezoneChanged) {
            eventPublisher.publishEvent(new UserTimezoneChangedEvent(user.getId(), currentTimezone, user.getTimezone()));
        }

        return null;
    }
}
