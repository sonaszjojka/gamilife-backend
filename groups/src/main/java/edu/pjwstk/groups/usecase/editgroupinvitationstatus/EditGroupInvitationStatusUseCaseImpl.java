package edu.pjwstk.groups.usecase.editgroupinvitationstatus;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.groups.entity.GroupInvitation;
import edu.pjwstk.groups.entity.InvitationStatus;
import edu.pjwstk.groups.exception.*;
import edu.pjwstk.groups.repository.GroupInvitationRepository;
import edu.pjwstk.groups.repository.InvitationStatusRepository;
import edu.pjwstk.groups.shared.GroupRequestStatusEnum;
import edu.pjwstk.groups.shared.InvitationStatusEnum;
import jakarta.validation.Valid;
import org.springframework.stereotype.Service;

import java.util.Objects;
import java.util.UUID;

@Service
public class EditGroupInvitationStatusUseCaseImpl implements EditGroupInvitationStatusUseCase {

    private final GroupInvitationRepository groupInvitationRepository;
    private final InvitationStatusRepository invitationStatusRepository;
    private final AuthApi authApi;
    private final EditGroupInvitationStatusMapper editGroupInvitationStatusMapper;

    public EditGroupInvitationStatusUseCaseImpl(GroupInvitationRepository groupInvitationRepository, InvitationStatusRepository invitationStatusRepository, AuthApi authApi, EditGroupInvitationStatusMapper editGroupInvitationStatusMapper) {
        this.groupInvitationRepository = groupInvitationRepository;
        this.invitationStatusRepository = invitationStatusRepository;
        this.authApi = authApi;
        this.editGroupInvitationStatusMapper = editGroupInvitationStatusMapper;
    }

    @Override
    public EditGroupInvitationStatusResponse execute(UUID groupInvitationId, EditGroupInvitationStatusRequest request) {
        GroupInvitation groupInvitation = groupInvitationRepository.findById(groupInvitationId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id: " + groupInvitationId
                        + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        //todo: validation below depends on implementation of accepting invitation
        if (!Objects.equals(currentUserDto.userId(), groupInvitation.getUserId())) {
            throw new UserNotOwnerAccessDeniedException("Only user who is assigned to this invitation can change group invitation status!");
        }

        if (groupInvitation.getInvitationStatus().toEnum() == InvitationStatusEnum.ACCEPTED
                || groupInvitation.getInvitationStatus().toEnum() == InvitationStatusEnum.DECLINED) {
            throw new InvalidGroupInvitationDataException("Group invitation with status ACCEPTED or DECLINED are final and cannot be changed!");
        }

        if (request.invitationStatus() == InvitationStatusEnum.SENT) {
            throw new InvalidGroupDataException("Group invitation with id: " + groupInvitationId + " has already status: SENT");
        }

        InvitationStatus invitationStatus = invitationStatusRepository.findById(request.invitationStatus().getId())
                .orElseThrow(() -> new InvitationStatusNotFoundException("Group invitation with id: " + groupInvitationId
                        + " not found!"));

        groupInvitation.setInvitationStatus(invitationStatus);
        GroupInvitation savedGroupInvitation = groupInvitationRepository.save(groupInvitation);
        return editGroupInvitationStatusMapper.toResponse(savedGroupInvitation);

    }
}
