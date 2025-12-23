package pl.gamilife.user.usecase.getusers;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class GetUsersUseCaseImpl implements GetUsersUseCase {

    private final UserRepository userRepository;

    @Override
    public pl.gamilife.shared.kernel.architecture.Page<GetUsersResult> execute(GetUsersCommand cmd) {
        Page<User> userPage = userRepository.findAll(
                cmd.username(),
                cmd.page(),
                cmd.size()
        );

        return userPage.map(u -> new GetUsersResult(
                u.getId(),
                u.getUsername(),
                u.isProfilePublic() ? u.getLevel() : null,
                u.isProfilePublic()
        ));
    }
}
