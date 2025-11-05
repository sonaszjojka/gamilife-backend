package edu.pjwstk.groups.usecase.creategroup;

import edu.pjwstk.api.groupshop.GroupShopApi;
import edu.pjwstk.api.groupshop.dto.CreateGroupShopForGroupRequestDto;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.exception.GroupTypeNotFoundException;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.repository.GroupTypeRepository;
import edu.pjwstk.groups.util.JoinCodeGenerator;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.UUID;

@Service
public class CreateGroupUseCaseImpl implements CreateGroupUseCase {

    private final GroupRepository groupRepository;
    private final CreateGroupMapper createGroupUseCaseMapper;
    private final UserApi userApi;
    private final GroupTypeRepository groupTypeRepository;
    private final GroupMemberRepository groupMemberRepository;
    private final JoinCodeGenerator joinCodeGenerator;
    private final GroupShopApi groupShopApi;

    public CreateGroupUseCaseImpl(GroupRepository groupRepository, UserApi userApi, CreateGroupMapper createGroupUseCaseMapper, GroupTypeRepository groupTypeRepository, GroupMemberRepository groupMemberRepository, JoinCodeGenerator joinCodeGenerator, GroupShopApi groupShopApi) {
        this.groupRepository = groupRepository;
        this.userApi = userApi;
        this.createGroupUseCaseMapper = createGroupUseCaseMapper;
        this.groupTypeRepository = groupTypeRepository;
        this.groupMemberRepository = groupMemberRepository;
        this.joinCodeGenerator = joinCodeGenerator;
        this.groupShopApi = groupShopApi;
    }

    @Override
    @Transactional
    public CreateGroupResponse execute(CreateGroupRequest request) {
        GroupType groupType = groupTypeRepository.findById(request.groupType().getId())
                .orElseThrow(() -> new GroupTypeNotFoundException("Group type with id: " +
                        request.groupType().getId() + " not found!"));

        Optional<BasicUserInfoApiDto> admin = userApi.getUserById(request.adminId());

        if (admin.isEmpty()) {
            throw new UserNotFoundException("User (admin) with id: " + request.adminId() + " not found!");
        }

        String joinCode = joinCodeGenerator.generate(20);
        Group group = createGroupUseCaseMapper.toEntity(request, joinCode, UUID.randomUUID(), groupType);
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
        return createGroupUseCaseMapper.toResponse(savedGroup);
    }
}
