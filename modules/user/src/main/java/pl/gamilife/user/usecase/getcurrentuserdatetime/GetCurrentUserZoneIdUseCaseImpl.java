package pl.gamilife.user.usecase.getcurrentuserdatetime;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.UserRepository;

import java.time.ZoneId;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetCurrentUserZoneIdUseCaseImpl implements GetCurrentUserZoneIdUseCase {

    private final UserRepository userRepository;

    @Override
    public ZoneId execute(GetCurrentUserZoneIdCommand cmd) {
        User user = userRepository.getUserById(cmd.userId()).orElseThrow(
                () -> new UserNotFoundException("User not found")
        );

        return ZoneId.of(user.getTimezone());
    }
}
