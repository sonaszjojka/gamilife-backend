package pl.gamilife.group.usecase.creategroup;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.CurrentUserDto;
import pl.gamilife.api.groupshop.GroupShopApi;
import pl.gamilife.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import pl.gamilife.group.exception.domain.GroupTypeNotFoundException;
import pl.gamilife.group.model.Group;
import pl.gamilife.group.model.GroupMember;
import pl.gamilife.group.model.GroupType;
import pl.gamilife.group.repository.GroupJpaRepository;
import pl.gamilife.group.repository.GroupMemberJpaRepository;
import pl.gamilife.group.repository.GroupTypeJpaRepository;
import pl.gamilife.group.util.JoinCodeGenerator;

import java.time.Instant;
import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class CreateGroupUseCaseImpl implements CreateGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;
    private final GroupTypeJpaRepository groupTypeRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final JoinCodeGenerator joinCodeGenerator;
    private final GroupShopApi groupShopApi;

    @Override
    public CreateGroupResult execute(CreateGroupCommand cmd) {
        GroupType groupType = getGroupType(cmd.groupTypeId());
        CurrentUserDto admin = authApi.getCurrentUser();

        Group group = createGroup(cmd, groupType, admin.userId());
        addGroupAdmin(group, admin.userId());
        groupShopApi.createGroupShopOnGroupInit(new CreateGroupShopForGroupRequestDto(
                group.getName() + "'s shop",
                "Default description",
                group.getGroupId()
        ));

        return buildCreateGroupResult(group);
    }

    private GroupType getGroupType(Integer groupTypeId) {
        return groupTypeRepository.findById(groupTypeId)
                .orElseThrow(() -> new GroupTypeNotFoundException("Group type with id: " +
                        groupTypeId + " not found!"));
    }

    private Group createGroup(CreateGroupCommand cmd, GroupType groupType, UUID adminUserId) {
        String joinCode = joinCodeGenerator.generate(20);
        Group group = Group.builder()
                .groupId(UUID.randomUUID())
                .name(cmd.groupName())
                .joinCode(joinCode)
                .adminId(adminUserId)
                .groupCurrencySymbol(cmd.groupCurrencySymbol())
                .membersLimit(cmd.membersLimit())
                .groupType(groupType)
                .build();

        return groupRepository.save(group);
    }

    private void addGroupAdmin(Group group, UUID adminUserId) {
        GroupMember groupMemberAdmin = GroupMember.builder()
                .groupMemberId(UUID.randomUUID())
                .group(group)
                .userId(adminUserId)
                .leftAt(null)
                .joinedAt(Instant.now())
                .groupMoney(0)
                .totalEarnedMoney(0)
                .build();

        groupMemberRepository.save(groupMemberAdmin);
    }

    private CreateGroupResult buildCreateGroupResult(Group group) {
        return CreateGroupResult.builder()
                .groupId(group.getGroupId())
                .groupName(group.getName())
                .joinCode(group.getJoinCode())
                .adminId(group.getAdminId())
                .groupCurrencySymbol(group.getGroupCurrencySymbol())
                .membersLimit(group.getMembersLimit())
                .groupType(new CreateGroupResult.GroupTypeDto(
                        group.getGroupType().getGroupTypeId(),
                        group.getGroupType().getTitle()
                ))
                .build();
    }
}
