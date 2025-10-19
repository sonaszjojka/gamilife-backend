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
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import edu.pjwstk.groups.usecase.creategroupmember.creategroupmemberafteracceptation.CreateGroupMemberAfterAcceptationRequest;
import edu.pjwstk.groups.usecase.creategroupmember.creategroupmemberafteracceptation.CreateGroupMemberAfterAcceptationUseCase;
import edu.pjwstk.groups.util.GroupInvitationUtil;
import jakarta.validation.Valid;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

@Service
public class EditGroupInvitationStatusUseCaseImpl implements EditGroupInvitationStatusUseCase {

    private final GroupInvitationRepository groupInvitationRepository;
    private final InvitationStatusRepository invitationStatusRepository;
    private final AuthApi authApi;
    private final EditGroupInvitationStatusMapper editGroupInvitationStatusMapper;
    private final CreateGroupMemberAfterAcceptationUseCase createGroupMemberAfterAcceptationUseCase;
    private final GroupInvitationUtil groupInvitationUtil;

    public EditGroupInvitationStatusUseCaseImpl(GroupInvitationRepository groupInvitationRepository, InvitationStatusRepository invitationStatusRepository, AuthApi authApi, EditGroupInvitationStatusMapper editGroupInvitationStatusMapper, CreateGroupMemberAfterAcceptationUseCase createGroupMemberAfterAcceptationUseCase, GroupInvitationUtil groupInvitationUtil) {
        this.groupInvitationRepository = groupInvitationRepository;
        this.invitationStatusRepository = invitationStatusRepository;
        this.authApi = authApi;
        this.editGroupInvitationStatusMapper = editGroupInvitationStatusMapper;
        this.createGroupMemberAfterAcceptationUseCase = createGroupMemberAfterAcceptationUseCase;
        this.groupInvitationUtil = groupInvitationUtil;
    }

    @Override
    @Transactional
    public EditGroupInvitationStatusResponse execute(UUID groupInvitationId, EditGroupInvitationStatusRequest request) {
        GroupInvitation groupInvitation = groupInvitationRepository.findById(groupInvitationId)
                .orElseThrow(() -> new GroupInvitationNotFoundException("Group invitation with id: " + groupInvitationId
                        + " not found!"));

        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        if (!Objects.equals(currentUserDto.userId(), groupInvitation.getUserId())) {
            throw new UserNotOwnerAccessDeniedException("Only user who is assigned to this invitation can change group invitation status!");
        }

        if (!groupInvitationUtil.verifyToken(request.token(), groupInvitation.getTokenHash())) {
            throw new InvalidGroupInvitationTokenException("Invalid or tampered group invitation token!");
        }

        if (LocalDateTime.now().isAfter(groupInvitation.getExpiresAt())) {
            throw new GroupInvitationExpiredException("Group invitation has expired at: "
                    + groupInvitation.getExpiresAt());
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

        CreateGroupMemberResponse createGroupMemberResponse = null;
        if (request.invitationStatus() == InvitationStatusEnum.ACCEPTED) {
            createGroupMemberResponse = createGroupMemberAfterAcceptationUseCase.execute(
                    CreateGroupMemberAfterAcceptationRequest.builder()
                            .groupId(groupInvitation.getGroupInvited().getGroupId())
                            .userId(groupInvitation.getUserId())
                            .build());
        }

        groupInvitation.setInvitationStatus(invitationStatus);
        GroupInvitation savedGroupInvitation = groupInvitationRepository.save(groupInvitation);
        return editGroupInvitationStatusMapper.toResponse(savedGroupInvitation, createGroupMemberResponse);
    }
}
