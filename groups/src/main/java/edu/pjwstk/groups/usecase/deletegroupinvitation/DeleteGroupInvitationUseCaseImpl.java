package edu.pjwstk.groups.usecase.deletegroupinvitation;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groups.exception.domain.GroupInvitationNotFoundException;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.repository.GroupInvitationJpaRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
@AllArgsConstructor
public class DeleteGroupInvitationUseCaseImpl implements DeleteGroupInvitationUseCase {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final AuthApi authApi;

    @Override
    @Transactional
    public Void executeInternal(DeleteGroupInvitationCommand cmd) {
        GroupInvitation groupInvitation = getGroupInvitation(cmd.groupId(), cmd.groupInvitationId());
        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!Objects.equals(currentUserDto.userId(), groupInvitation.getGroupInvited().getAdminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete group invitations!");
        }

        groupInvitationRepository.deleteById(cmd.groupInvitationId());

        return null;
    }

    private GroupInvitation getGroupInvitation(UUID groupId, UUID groupInvitationId) {
        return groupInvitationRepository.findByGroupInvitationIdAndGroupInvited_GroupId(groupInvitationId, groupId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id:"
                        + groupInvitationId + " not found!"));
    }
}
