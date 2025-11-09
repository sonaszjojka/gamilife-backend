package edu.pjwstk.groups.usecase.creategroup;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.CurrentUserDto;
import edu.pjwstk.api.groupshop.GroupShopApi;
import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.groups.exception.domain.GroupTypeNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.model.GroupType;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import edu.pjwstk.groups.repository.GroupTypeJpaRepository;
import edu.pjwstk.groups.util.JoinCodeGenerator;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@AllArgsConstructor
public class CreateGroupUseCaseImpl implements CreateGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final AuthApi authApi;
    private final GroupTypeJpaRepository groupTypeRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final JoinCodeGenerator joinCodeGenerator;
    private final GroupShopApi groupShopApi;

    @Override
    @Transactional
    public CreateGroupResult executeInternal(CreateGroupCommand cmd) {
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
                .memberGroup(group)
                .userId(adminUserId)
                .leftAt(null)
                .groupMoney(0)
                .totalEarnedMoney(0)
                .build();

        groupMemberRepository.save(groupMemberAdmin);
    }

    private CreateGroupResult buildCreateGroupResult(Group group) {
        CreateGroupResult.GroupTypeDto groupTypeDto = group.getGroupType() != null ?
                new CreateGroupResult.GroupTypeDto(
                        group.getGroupType().getGroupTypeId(),
                        group.getGroupType().getTitle()
                ) : null;

        return CreateGroupResult.builder()
                .groupId(group.getGroupId())
                .groupName(group.getName())
                .joinCode(group.getJoinCode())
                .adminId(group.getAdminId())
                .groupCurrencySymbol(group.getGroupCurrencySymbol())
                .membersLimit(group.getMembersLimit())
                .groupType(groupTypeDto)
                .build();
    }
}
