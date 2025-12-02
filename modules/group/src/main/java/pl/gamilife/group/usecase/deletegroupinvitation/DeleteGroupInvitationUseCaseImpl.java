package pl.gamilife.group.usecase.deletegroupinvitation;

import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.infrastructure.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import pl.gamilife.group.exception.domain.GroupInvitationNotFoundException;
import pl.gamilife.group.model.GroupInvitation;
import pl.gamilife.group.repository.GroupInvitationJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class DeleteGroupInvitationUseCaseImpl implements DeleteGroupInvitationUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final AuthApi authApi;

    @Override
    public Void execute(DeleteGroupInvitationCommand cmd) {
        GroupInvitation groupInvitation = getGroupInvitationWithGroup(cmd.groupId(), cmd.groupInvitationId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!groupInvitation.getGroup().isUserAdmin(currentUserDto.userId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete group invitations!");
        }

        groupInvitationRepository.deleteById(cmd.groupInvitationId());

        return null;
    }

    private GroupInvitation getGroupInvitationWithGroup(UUID groupId, UUID groupInvitationId) {
        return groupInvitationRepository.findWithGroupByGroupInvitationIdAndGroupId(groupInvitationId, groupId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id:"
                        + groupInvitationId + " not found!"));
    }
}
