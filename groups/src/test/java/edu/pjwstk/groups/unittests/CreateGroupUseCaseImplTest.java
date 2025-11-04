package edu.pjwstk.groups.unittests;

import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupMember;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.exception.GroupTypeNotFoundException;
import edu.pjwstk.groups.repository.GroupMemberRepository;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.repository.GroupTypeRepository;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupMapper;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupRequest;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupResponse;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupUseCaseImpl;
import edu.pjwstk.groups.util.JoinCodeGenerator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CreateGroupUseCaseImplTest {

    @Mock
    private GroupRepository groupRepository;

    @Mock
    private CreateGroupMapper createGroupMapper;

    @Mock
    private UserApi userApi;

    @Mock
    private GroupTypeRepository groupTypeRepository;

    @Mock
    private GroupMemberRepository groupMemberRepository;

    @Mock
    private JoinCodeGenerator joinCodeGenerator;

    @InjectMocks
    private CreateGroupUseCaseImpl createGroupUseCase;

    private UUID adminId;
    private CreateGroupRequest request;
    private GroupType groupType;
    private Group group;
    private BasicUserInfoApiDto adminUser;
    private CreateGroupResponse response;

    @BeforeEach
    void setUp() {
        adminId = UUID.randomUUID();

        request = new CreateGroupRequest(
                adminId,
                '$',
                GroupTypeEnum.OPEN,
                10
        );

        groupType = new GroupType();
        groupType.setGroupTypeId(GroupTypeEnum.OPEN.getId());
        groupType.setTitle("Family");

        group = new Group();
        group.setGroupId(UUID.randomUUID());
        group.setJoinCode("ABCDEFGHIJ1234567890");
        group.setAdminId(adminId);
        group.setGroupCurrencySymbol('$');
        group.setMembersLimit(10);
        group.setGroupType(groupType);

        adminUser = new BasicUserInfoApiDto(adminId, "admin@example.com", "AdminUser");

        CreateGroupResponse.GroupTypeDto groupTypeDto = new CreateGroupResponse.GroupTypeDto(
                GroupTypeEnum.OPEN.getId(),
                "Family"
        );

        response = CreateGroupResponse.builder()
                .groupId(group.getGroupId())
                .joinCode("ABCDEFGHIJ1234567890")
                .adminId(adminId)
                .groupCurrencySymbol('$')
                .membersLimit(10)
                .groupType(groupTypeDto)
                .build();
    }

    @Test
    void shouldSuccessfullyCreateGroup_whenValidRequest() {
        // Given
        when(groupTypeRepository.findById(GroupTypeEnum.OPEN.getId())).thenReturn(Optional.of(groupType));
        when(userApi.getUserById(adminId)).thenReturn(Optional.of(adminUser));
        when(joinCodeGenerator.generate(20)).thenReturn("ABCDEFGHIJ1234567890");
        when(createGroupMapper.toEntity(eq(request), anyString(), any(UUID.class), eq(groupType)))
                .thenReturn(group);
        when(groupRepository.save(group)).thenReturn(group);
        when(groupMemberRepository.save(any(GroupMember.class))).thenReturn(new GroupMember());
        when(createGroupMapper.toResponse(group)).thenReturn(response);

        // When
        CreateGroupResponse result = createGroupUseCase.execute(request);

        // Then
        assertNotNull(result);
        assertEquals(response, result);
        assertEquals(group.getGroupId(), result.groupId());
        assertEquals("ABCDEFGHIJ1234567890", result.joinCode());
        assertEquals(adminId, result.adminId());
        assertEquals('$', result.groupCurrencySymbol());
        assertEquals(10, result.membersLimit());
        assertNotNull(result.groupType());
        assertEquals(GroupTypeEnum.OPEN.getId(), result.groupType().groupTypeId());
        assertEquals("Family", result.groupType().title());

        verify(groupTypeRepository).findById(GroupTypeEnum.OPEN.getId());
        verify(userApi).getUserById(adminId);
        verify(joinCodeGenerator).generate(20);
        verify(createGroupMapper).toEntity(eq(request), eq("ABCDEFGHIJ1234567890"), any(UUID.class), eq(groupType));
        verify(groupRepository).save(group);
        verify(createGroupMapper).toResponse(group);

        ArgumentCaptor<GroupMember> groupMemberCaptor = ArgumentCaptor.forClass(GroupMember.class);
        verify(groupMemberRepository).save(groupMemberCaptor.capture());
        GroupMember savedGroupMember = groupMemberCaptor.getValue();
        assertNotNull(savedGroupMember);
        assertEquals(group, savedGroupMember.getMemberGroup());
        assertEquals(adminId, savedGroupMember.getUserId());
        assertNull(savedGroupMember.getLeftAt());
        assertEquals(0, savedGroupMember.getGroupMoney());
        assertEquals(0, savedGroupMember.getTotalEarnedMoney());
    }

    @Test
    void shouldThrowGroupTypeNotFoundException_whenGroupTypeDoesNotExist() {
        // Given
        when(groupTypeRepository.findById(GroupTypeEnum.OPEN.getId())).thenReturn(Optional.empty());

        // When
        GroupTypeNotFoundException exception = assertThrows(
                GroupTypeNotFoundException.class,
                () -> createGroupUseCase.execute(request)
        );

        // Then
        assertEquals("Group type with id: " + GroupTypeEnum.OPEN.getId() + " not found!", exception.getMessage());
        verify(groupTypeRepository).findById(GroupTypeEnum.OPEN.getId());
        verify(userApi, never()).getUserById(any());
        verify(groupRepository, never()).save(any());
        verify(groupMemberRepository, never()).save(any());
    }

    @Test
    void shouldThrowUserNotFoundException_whenAdminUserDoesNotExist() {
        // Given
        when(groupTypeRepository.findById(GroupTypeEnum.OPEN.getId())).thenReturn(Optional.of(groupType));
        when(userApi.getUserById(adminId)).thenReturn(Optional.empty());

        // When
        UserNotFoundException exception = assertThrows(
                UserNotFoundException.class,
                () -> createGroupUseCase.execute(request)
        );

        // Then
        assertEquals("User (admin) with id: " + adminId + " not found!", exception.getMessage());
        verify(groupTypeRepository).findById(GroupTypeEnum.OPEN.getId());
        verify(userApi).getUserById(adminId);
        verify(groupRepository, never()).save(any());
        verify(groupMemberRepository, never()).save(any());
    }
}