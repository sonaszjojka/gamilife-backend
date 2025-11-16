package edu.pjwstk.groups.usecase.getgroups.getbyid;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.exception.common.domain.GroupNotFoundException;
import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import edu.pjwstk.groups.repository.GroupRequestJpaRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class GetGroupByIdUseCaseImpl implements GetGroupByIdUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupRequestJpaRepository groupRequestJpaRepository;
    private final AuthApi authApi;
    private final UserApi userApi;

    @Override
    @Transactional(readOnly = true)
    public GetGroupByIdResult executeInternal(GetGroupByIdCommand cmd) {
        Group group = getGroupById(cmd.groupId());
        CurrentUserDto currentUser = getCurrentUser();

        if (Boolean.TRUE.equals(cmd.isForLoggedUser())) {
            Optional<GroupMember> memberOpt =
                    groupMemberRepository.findByUserIdAndGroupAndLeftAtIsNull(currentUser.userId(), group);

            boolean isMember = memberOpt.isPresent();
            boolean hasRequest = !isMember && hasActiveGroupRequest(currentUser.userId(), group);

            return buildGetGroupByIdResult(
                    group,
                    isMember,
                    hasRequest,
                    memberOpt.orElse(null)
            );
        }

        return buildGetGroupByIdResult(group, null, null, null);
    }

    private Group getGroupById(UUID groupId) {
        return groupRepository.findById(groupId)
                .orElseThrow(() -> new GroupNotFoundException("Group with id: " + groupId + " not found!"));
    }

    private CurrentUserDto getCurrentUser() {
        return authApi.getCurrentUser();
    }

    private boolean hasActiveGroupRequest(UUID userId, Group group) {
        return groupRequestJpaRepository.existsByGroupRequestedAndUserIdAndGroupRequestStatusId(
                group, userId, GroupRequestStatusEnum.SENT.getId()
        );
    }

    private GetGroupByIdResult buildGetGroupByIdResult(
            Group group,
            Boolean isMember,
            Boolean hasActiveRequest,
            GroupMember loggedUserMembership
    ) {
        return new GetGroupByIdResult(
                group.getGroupId(),
                group.getJoinCode(),
                group.getName(),
                group.getAdminId(),
                group.getGroupCurrencySymbol(),
                group.getMembersLimit(),
                new GetGroupByIdResult.GroupTypeDto(group.getGroupType().getGroupTypeId(), group.getGroupType().getTitle()),
                group.getGroupMembers().size(),
                isMember,
                hasActiveRequest,
                buildGroupMemberDto(loggedUserMembership),
                mapMembers(group)
        );
    }

    private GetGroupByIdResult.GroupMemberDto buildGroupMemberDto(GroupMember gm) {
        if (gm == null) return null;

        return new GetGroupByIdResult.GroupMemberDto(
                gm.getGroupMemberId(),
                gm.getGroup().getGroupId(),
                gm.getUserId(),
                gm.getGroupMoney(),
                gm.getTotalEarnedMoney(),
                gm.getJoinedAt(),
                gm.getLeftAt(),
                getUsernameByUserId(gm.getUserId())
        );
    }

    private Collection<GetGroupByIdResult.GroupMemberDto> mapMembers(Group group) {
        return group.getGroupMembers()
                .stream()
                .map(this::buildGroupMemberDto)
                .toList();
    }

    private String getUsernameByUserId(UUID userId) {
        return userApi.getUserById(userId)
                .map(BasicUserInfoApiDto::username)
                .orElse(null);
    }

}
