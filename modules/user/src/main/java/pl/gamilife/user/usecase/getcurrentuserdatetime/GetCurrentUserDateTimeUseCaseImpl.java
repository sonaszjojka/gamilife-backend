package pl.gamilife.user.usecase.getcurrentuserdatetime;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;

import java.time.LocalDateTime;
import java.time.ZoneId;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetCurrentUserDateTimeUseCaseImpl implements GetCurrentUserDateTimeUseCase {

    private final UserRepository userRepository;

    @Override
    public LocalDateTime execute(GetCurrentUserDateTimeCommand cmd) {
        User user = userRepository.getUserById(cmd.userId()).orElseThrow(
                () -> new UserNotFoundException("User not found")
        );

        ZoneId zoneId = ZoneId.of(user.getTimezone());

        return LocalDateTime.now(zoneId);
    }
}
