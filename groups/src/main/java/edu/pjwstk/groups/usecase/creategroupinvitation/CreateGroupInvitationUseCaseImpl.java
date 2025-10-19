package edu.pjwstk.groups.usecase.creategroupinvitation;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupInvitation;
import edu.pjwstk.groups.entity.InvitationStatus;
import edu.pjwstk.groups.exception.GroupFullException;
import edu.pjwstk.groups.exception.InvitationStatusNotFoundException;
import edu.pjwstk.groups.exception.UserNotGroupAdministratorAccessDeniedException;
import edu.pjwstk.groups.repository.GroupInvitationRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.repository.InvitationStatusRepository;
import edu.pjwstk.groups.shared.InvitationStatusEnum;
import edu.pjwstk.groups.usecase.creategroupmember.creategroupmemberafteracceptation.CreateGroupMemberAfterAcceptationUseCase;
import edu.pjwstk.groups.util.GroupInvitationUtil;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;
import java.util.UUID;

@Service
public class CreateGroupInvitationUseCaseImpl implements CreateGroupInvitationUseCase {

    private final GroupInvitationRepository groupInvitationRepository;
    private final InvitationStatusRepository invitationStatusRepository;
    private final GroupRepository groupRepository;
    private final CreateGroupMemberAfterAcceptationUseCase createGroupMemberAfterAcceptationUseCase;
    private final UserApi userApi;
    private final AuthApi authApi;
    private final CreateGroupInvitationStatusMapper createGroupInvitationStatusMapper;
    private final GroupInvitationUtil invitationStatusUtil;

    public CreateGroupInvitationUseCaseImpl(GroupInvitationRepository groupInvitationRepository, InvitationStatusRepository invitationStatusRepository, GroupRepository groupRepository, CreateGroupMemberAfterAcceptationUseCase createGroupMemberAfterAcceptationUseCase, UserApi userApi, AuthApi authApi, CreateGroupInvitationStatusMapper createGroupInvitationStatusMapper, GroupInvitationUtil invitationStatusUtil) {
        this.groupInvitationRepository = groupInvitationRepository;
        this.invitationStatusRepository = invitationStatusRepository;
        this.groupRepository = groupRepository;
        this.createGroupMemberAfterAcceptationUseCase = createGroupMemberAfterAcceptationUseCase;
        this.userApi = userApi;
        this.authApi = authApi;
        this.createGroupInvitationStatusMapper = createGroupInvitationStatusMapper;
        this.invitationStatusUtil = invitationStatusUtil;
    }

    @Override
    @Transactional
    public CreateGroupInvitationResponse execute(UUID groupId, CreateGroupInvitationRequest request) {
        CurrentUserDto currentUserDto = authApi.getCurrentUser()
                .orElseThrow();

        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id:" + groupId + " not found!"));

        if (!Objects.equals(currentUserDto.userId(), group.getAdminId())) {
            throw new UserNotGroupAdministratorAccessDeniedException("Only group administrators " +
                    "can create group invitations!");
        }

        if (group.getGroupMembers().size() >= group.getMembersLimit()) {
            throw new GroupFullException("Group with id: " + groupId + " is full!");
        }

        BasicUserInfoApiDto basicUserInfoApiDto = userApi.getUserById(request.userId())
                .orElseThrow(() -> new UserNotFoundException("User with id: " + request.userId() + " not found!"));

        InvitationStatus invitationStatus = invitationStatusRepository.findById(InvitationStatusEnum.SENT.getId())
                .orElseThrow(() -> new InvitationStatusNotFoundException("Invitation stauts with id: "
                        + InvitationStatusEnum.SENT.getId() + " not found!"));

        UUID groupInvitationId = UUID.randomUUID();

        // send email etc. - integration

        GroupInvitation groupInvitation = createGroupInvitationStatusMapper.toEntity(
                group,
                invitationStatus,
                basicUserInfoApiDto.userId(),
                invitationStatusUtil.calculateExpirationDate(),
                invitationStatusUtil.generateGroupInvitationLink(groupId, groupInvitationId),
                groupInvitationId);
        GroupInvitation savedGroupInvitation = groupInvitationRepository.save(groupInvitation);

        return createGroupInvitationStatusMapper.toResponse(savedGroupInvitation);
    }
}
