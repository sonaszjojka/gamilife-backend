package pl.gamilife.group.usecase.getgrouptimezone;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;

import java.time.ZoneId;

@Service
@AllArgsConstructor
public class GetGroupTimezoneUseCaseImpl implements GetGroupTimezoneUseCase {

    private final GroupJpaRepository groupRepository;

    @Override
    public ZoneId execute(GetGroupTimezoneCommand cmd) {
        Group group = groupRepository.findById(cmd.groupId()).orElseThrow(
                () -> new GroupNotFoundException("Group not found")
        );

        return ZoneId.of(group.getTimezone());
    }
}
