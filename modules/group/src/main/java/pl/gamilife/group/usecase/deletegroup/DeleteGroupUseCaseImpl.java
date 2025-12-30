package pl.gamilife.group.usecase.deletegroup;

import lombok.AllArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.shared.kernel.event.GroupDeletionRequestedEvent;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;

@Service
@Transactional
@AllArgsConstructor
public class DeleteGroupUseCaseImpl implements DeleteGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Override
    public Void execute(DeleteGroupCommand cmd) {
        Group group = groupRepository.findById(cmd.groupId())
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + cmd.groupId() + " not found!"));

        if (!group.isUserAdmin(cmd.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete groups!");
        }

        eventPublisher.publishEvent(new GroupDeletionRequestedEvent(cmd.groupId()));

        groupRepository.deleteById(cmd.groupId());

        return null;
    }
}
