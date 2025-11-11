package edu.pjwstk.groups.usecase.getgroups.getbyid;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class GetGroupByIdUseCaseImpl implements GetGroupByIdUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupRequestJpaRepository groupRequestJpaRepository;
    private final AuthApi authApi;

    @Override
    public GetGroupByIdResult executeInternal(GetGroupByIdCommand cmd) {
        Group group = getGroupById(cmd.groupId());
        CurrentUserDto currentUser = getCurrentUser();

        if(cmd.isForLoggedUser() != null && cmd.isForLoggedUser()) {
            boolean isGroupMember = checkGroupMembership(currentUser.userId(), group);
            boolean hasActiveGroupRequest = !isGroupMember && hasActiveGroupRequest(currentUser.userId(), group);
            return buildGetGroupByIdResult(group, isGroupMember, hasActiveGroupRequest);
        }
        return buildGetGroupByIdResult(group, null, null);
    }

    private Group getGroupById(UUID groupId) {
        log.debug("Fetching group by id: {}", groupId);
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
        log.debug("Found group with id: {}", group.getGroupId());
        return group;
    }

    private CurrentUserDto getCurrentUser() {
        CurrentUserDto currentUser = authApi.getCurrentUser();
        log.debug("Fetched current user with id: {}", currentUser.userId());
        return currentUser;
    }

    private boolean checkGroupMembership(UUID userId, Group group) {
        boolean isMember = groupMemberRepository.existsByUserIdAndGroup(userId, group);
        log.debug("User with id: {} is {}member of group with id: {}",
                userId, isMember ? "" : "not ", group.getGroupId());
        return isMember;
    }

    private boolean hasActiveGroupRequest(UUID userId, Group group) {
        boolean hasActiveGroupRequest = groupRequestJpaRepository
                .existsByGroupRequestedAndUserIdAndGroupRequestStatusId(
                        group,
                        userId,
                        GroupRequestStatusEnum.SENT.getId()
                );
        log.debug("User with id: {} has {}active request for group with id: {}",
                userId, hasActiveGroupRequest ? "" : "no ", group.getGroupId());
        return hasActiveGroupRequest;
    }

    private GetGroupByIdResult buildGetGroupByIdResult(
            Group group,
             Boolean isGroupMember,
             Boolean hasActiveGroupRequest
    ) {
        return new GetGroupByIdResult(
                group.getGroupId(),
                group.getJoinCode(),
                group.getName(),
                group.getAdminId(),
                group.getGroupCurrencySymbol(),
                group.getMembersLimit(),
                new GetGroupByIdResult.GroupTypeDto(group.getGroupType().getTitle()),
                group.getGroupMembers().size(),
                isGroupMember,
                hasActiveGroupRequest
        );
    }
}
