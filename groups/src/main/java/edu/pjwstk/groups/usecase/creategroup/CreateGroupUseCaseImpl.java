package edu.pjwstk.groups.usecase.creategroup;

import edu.pjwstk.api.groupshop.GroupShopApi;
import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.groups.exception.domain.GroupTypeNotFoundException;
import edu.pjwstk.groups.model.Group;
import edu.pjwstk.groups.model.GroupMember;
import edu.pjwstk.groups.model.GroupType;
import edu.pjwstk.groups.repository.GroupJpaRepository;
import edu.pjwstk.groups.repository.GroupMemberJpaRepository;
import edu.pjwstk.groups.repository.GroupTypeJpaRepository;
import edu.pjwstk.groups.util.JoinCodeGenerator;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Service
public class CreateGroupUseCaseImpl implements CreateGroupUseCase {

    private final GroupJpaRepository groupRepository;
    private final UserApi userApi;
    private final GroupTypeJpaRepository groupTypeRepository;
    private final GroupMemberJpaRepository groupMemberRepository;
    private final JoinCodeGenerator joinCodeGenerator;
    private final GroupShopApi groupShopApi;

    public CreateGroupUseCaseImpl(GroupJpaRepository groupRepository,
                                  UserApi userApi,
                                  GroupTypeJpaRepository groupTypeRepository,
                                  GroupMemberJpaRepository groupMemberRepository,
                                  JoinCodeGenerator joinCodeGenerator,
                                  GroupShopApi groupShopApi) {
        this.groupRepository = groupRepository;
        this.userApi = userApi;
        this.groupTypeRepository = groupTypeRepository;
        this.groupMemberRepository = groupMemberRepository;
        this.joinCodeGenerator = joinCodeGenerator;
        this.groupShopApi = groupShopApi;
    }

    @Override
    @Transactional
    public CreateGroupResult executeInternal(CreateGroupCommand cmd) {
        GroupType groupType = groupTypeRepository.findById(cmd.groupTypeId())
                .orElseThrow(() -> new GroupTypeNotFoundException("Group type with id: " +
                        cmd.groupTypeId() + " not found!"));

        Optional<BasicUserInfoApiDto> admin = userApi.getUserById(cmd.adminId());

        if (admin.isEmpty()) {
            throw new UserNotFoundException("User (admin) with id: " + cmd.adminId() + " not found!");
        }

        String joinCode = joinCodeGenerator.generate(20);
        Group group = Group.builder()
                .groupId(UUID.randomUUID())
                .name(cmd.groupName())
                .joinCode(joinCode)
                .adminId(cmd.adminId())
                .groupCurrencySymbol(cmd.groupCurrencySymbol())
                .membersLimit(cmd.membersLimit())
                .groupType(groupType)
                .build();
        Group savedGroup = groupRepository.save(group);

        GroupMember groupMemberAdmin = GroupMember.builder()
                .groupMemberId(UUID.randomUUID())
                .memberGroup(group)
                .userId(admin.get().userId())
                .leftAt(null)
                .groupMoney(0)
                .totalEarnedMoney(0)
                .build();

        CreateGroupShopForGroupRequestDto groupShopOnInitRequest = new CreateGroupShopForGroupRequestDto(
                group.getGroupId() + " Group shop",
                "Default description",
                savedGroup.getGroupId()
        );
        groupShopApi.createGroupShopOnGroupInit(groupShopOnInitRequest);

        groupMemberRepository.save(groupMemberAdmin);

        CreateGroupResult.GroupTypeDto groupTypeDto = null;
        if (savedGroup.getGroupType() != null) {
            groupTypeDto = new CreateGroupResult.GroupTypeDto(
                    savedGroup.getGroupType().getGroupTypeId(),
                    savedGroup.getGroupType().getTitle()
            );
        }

        return CreateGroupResult.builder()
                .groupId(savedGroup.getGroupId())
                .groupName(savedGroup.getName())
                .joinCode(savedGroup.getJoinCode())
                .adminId(savedGroup.getAdminId())
                .groupCurrencySymbol(savedGroup.getGroupCurrencySymbol())
                .membersLimit(savedGroup.getMembersLimit())
                .groupType(groupTypeDto)
                .build();
    }
}
