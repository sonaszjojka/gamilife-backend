package pl.gamilife.group.usecase.getgroups.getbyid;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.user.UserApi;
import pl.gamilife.api.user.dto.BasicUserInfoApiDto;
import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.group.repository.GroupRequestJpaRepository;
import pl.gamilife.shared.kernel.exception.domain.GroupNotFoundException;

import java.util.*;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
@Slf4j
public class GetGroupByIdUseCaseImpl implements GetGroupByIdUseCase {

    private final GroupJpaRepository groupRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final GroupRequestJpaRepository groupRequestJpaRepository;
    private final AuthApi authApi;
    private final UserApi userApi;

    @Override
    public GetGroupByIdResult execute(GetGroupByIdCommand cmd) {
        Group group = getGroupById(cmd.groupId());
        CurrentUserDto currentUser = getCurrentUser();

        if (Boolean.TRUE.equals(cmd.isForLoggedUser())) {
            Optional<GroupMember> memberOpt =
                    groupMemberRepository.findActiveMember(currentUser.userId(), group);

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
        return groupRepository.findWithGroupMembersAndGroupTypeByGroupId(groupId)
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
        List<GetGroupByIdResult.GroupMemberDto> activeMembers = getActiveMembers(group);
        return new GetGroupByIdResult(
                group.getGroupId(),
                group.getJoinCode(),
                group.getName(),
                group.getAdminId(),
                group.getGroupCurrencySymbol(),
                group.getMembersLimit(),
                new GetGroupByIdResult.GroupTypeDto(
                        group.getGroupType().getGroupTypeId(),
                        group.getGroupType().getTitle()
                ),
                activeMembers.size(),
                isMember,
                hasActiveRequest,
                buildGroupMemberDto(loggedUserMembership),
                activeMembers,
                getAdminUsername(group.getAdminId())
        );
    }

    private List<GetGroupByIdResult.GroupMemberDto> getActiveMembers(Group group) {
        return mapMembers(group).stream()
                .filter(m -> m.leftAt() == null)
                .sorted(Comparator.comparing(GetGroupByIdResult.GroupMemberDto::totalEarnedMoney))
                .toList();
    }

    private String getAdminUsername(UUID adminId) {
        return userApi.getUserById(adminId)
                .map(BasicUserInfoApiDto::username)
                .orElse(null);
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
