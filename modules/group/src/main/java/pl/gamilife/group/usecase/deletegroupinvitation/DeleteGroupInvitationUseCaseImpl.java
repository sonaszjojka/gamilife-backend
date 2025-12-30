package pl.gamilife.group.usecase.deletegroupinvitation;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.group.exception.domain.GroupInvitationNotFoundException;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupAdminPrivilegesRequiredException;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class DeleteGroupInvitationUseCaseImpl implements DeleteGroupInvitationUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;

    @Override
    public Void execute(DeleteGroupInvitationCommand cmd) {
        GroupInvitation groupInvitation = getGroupInvitationWithGroup(cmd.groupId(), cmd.groupInvitationId());

        if (!groupInvitation.getGroup().isUserAdmin(cmd.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete group invitations!");
        }

        groupInvitationRepository.deleteById(cmd.groupInvitationId());

        return null;
    }

    private GroupInvitation getGroupInvitationWithGroup(UUID groupId, UUID groupInvitationId) {
        return groupInvitationRepository.findWithGroupByIdAndGroupId(groupInvitationId, groupId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id:"
                        + groupInvitationId + " not found!"));
    }
}
