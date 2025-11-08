package edu.pjwstk.groups.usecase.deletegroupinvitation;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.groups.model.GroupInvitation;
import edu.pjwstk.groups.exception.domain.GroupInvitationNotFoundException;
import edu.pjwstk.core.exception.common.domain.GroupAdminPrivilegesRequiredException;
import edu.pjwstk.groups.repository.GroupInvitationJpaRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class DeleteGroupInvitationByIdImpl implements DeleteGroupInvitationById {

    private final GroupInvitationJpaRepository groupInvitationRepository;
    private final AuthApi authApi;

    public DeleteGroupInvitationByIdImpl(GroupInvitationJpaRepository groupInvitationRepository, AuthApi authApi) {
        this.groupInvitationRepository = groupInvitationRepository;
        this.authApi = authApi;
    }

    @Override
    @Transactional
    public void execute(UUID groupInvitationId) {
        GroupInvitation groupInvitation = groupInvitationRepository.findById(groupInvitationId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id:"
                        + groupInvitationId + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser();

        if (!Objects.equals(currentUserDto.userId(), groupInvitation.getGroupInvited().getAdminId())) {
            throw new GroupAdminPrivilegesRequiredException("Only group administrators can delete group invitations!");
        }

        groupInvitationRepository.deleteById(groupInvitationId);
    }
}
