package edu.pjwstk.groups.unittests;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.exception.GroupNotFoundException;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import edu.pjwstk.groups.entity.Group;
import edu.pjwstk.groups.entity.GroupType;
import edu.pjwstk.groups.exception.GroupTypeNotFoundException;
import edu.pjwstk.groups.exception.UserNotGroupAdministratorAccessDeniedException;
import edu.pjwstk.groups.repository.GroupRepository;
import edu.pjwstk.groups.repository.GroupTypeRepository;
import edu.pjwstk.groups.shared.GroupTypeEnum;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupMapper;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupRequest;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupResponse;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupUseCaseImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class UpdateGroupUseCaseImplTest {

    @Mock
    private GroupRepository groupRepository;

    @Mock
    private GroupTypeRepository groupTypeRepository;

    @Mock
    private UserApi userApi;

    @Mock
    private UpdateGroupMapper updateGroupMapper;

    @Mock
    private AuthApi authApi;

    @InjectMocks
    private UpdateGroupUseCaseImpl updateGroupUseCase;

    private UUID groupId;
    private UUID adminId;
    private UUID newAdminId;
    private UpdateGroupRequest request;
    private GroupType groupType;
    private Group group;
    private BasicUserInfoApiDto newAdminUser;
    private CurrentUserDto currentUserDto;
    private UpdateGroupResponse response;

    @BeforeEach
    void setUp() {
        groupId = UUID.randomUUID();
        adminId = UUID.randomUUID();
        newAdminId = UUID.randomUUID();

        request = new UpdateGroupRequest(
                newAdminId,
                '€',
                GroupTypeEnum.OPEN,
                15
        );

        groupType = new GroupType();
        groupType.setGroupTypeId(GroupTypeEnum.OPEN.getId());
        groupType.setTitle("Family");

        group = new Group();
        group.setGroupId(groupId);
        group.setJoinCode("ABCDEFGHIJ1234567890");
        group.setAdminId(adminId);
        group.setGroupCurrencySymbol('$');
        group.setMembersLimit(10);
        group.setGroupType(groupType);

        newAdminUser = new BasicUserInfoApiDto(newAdminId, "newadmin@example.com", "NewAdmin");
        currentUserDto = new CurrentUserDto(adminId, "admin@example.com");

        UpdateGroupResponse.GroupTypeDto groupTypeDto = new UpdateGroupResponse.GroupTypeDto(
                GroupTypeEnum.OPEN.getId(),
                "Family"
        );

        response = UpdateGroupResponse.builder()
                .groupId(groupId)
                .joinCode("ABCDEFGHIJ1234567890")
                .adminId(newAdminId)
                .groupCurrencySymbol('€')
                .membersLimit(15)
                .groupType(groupTypeDto)
                .build();
    }

    @Test
    void shouldSuccessfullyUpdateGroup_whenValidRequest() {
        // Given
        when(authApi.getCurrentUser()).thenReturn(Optional.of(currentUserDto));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(userApi.getUserById(newAdminId)).thenReturn(Optional.of(newAdminUser));
        when(groupTypeRepository.findById(GroupTypeEnum.OPEN.getId())).thenReturn(Optional.of(groupType));
        when(groupRepository.save(group)).thenReturn(group);
        when(updateGroupMapper.toResponse(group)).thenReturn(response);

        // When
        UpdateGroupResponse result = updateGroupUseCase.execute(request, groupId);

        // Then
        assertNotNull(result);
        assertEquals(response, result);
        assertEquals(groupId, result.groupId());
        assertEquals("ABCDEFGHIJ1234567890", result.joinCode());
        assertEquals(newAdminId, result.adminId());
        assertEquals('€', result.groupCurrencySymbol());
        assertEquals(15, result.membersLimit());
        assertNotNull(result.groupType());
        assertEquals(GroupTypeEnum.OPEN.getId(), result.groupType().groupTypeId());
        assertEquals("Family", result.groupType().title());

        // Verify group was updated
        assertEquals('€', group.getGroupCurrencySymbol());
        assertEquals(15, group.getMembersLimit());
        assertEquals(newAdminId, group.getAdminId());
        assertEquals(groupType, group.getGroupType());

        verify(authApi).getCurrentUser();
        verify(groupRepository).findById(groupId);
        verify(userApi).getUserById(newAdminId);
        verify(groupTypeRepository).findById(GroupTypeEnum.OPEN.getId());
        verify(groupRepository).save(group);
        verify(updateGroupMapper).toResponse(group);
    }

    @Test
    void shouldThrowGroupNotFoundException_whenGroupDoesNotExist() {
        // Given
        when(authApi.getCurrentUser()).thenReturn(Optional.of(currentUserDto));
        when(groupRepository.findById(groupId)).thenReturn(Optional.empty());

        // When
        GroupNotFoundException exception = assertThrows(
                GroupNotFoundException.class,
                () -> updateGroupUseCase.execute(request, groupId)
        );

        // Then
        assertEquals("Group with id:" + groupId + " not found!", exception.getMessage());
        verify(authApi).getCurrentUser();
        verify(groupRepository).findById(groupId);
        verify(userApi, never()).getUserById(any());
        verify(groupTypeRepository, never()).findById(any());
        verify(groupRepository, never()).save(any());
    }

    @Test
    void shouldThrowUserNotGroupAdministratorAccessDeniedException_whenUserIsNotAdmin() {
        // Given
        UUID unauthorizedUserId = UUID.randomUUID();
        CurrentUserDto unauthorizedUser = new CurrentUserDto(unauthorizedUserId, "user@example.com");

        when(authApi.getCurrentUser()).thenReturn(Optional.of(unauthorizedUser));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));

        // When
        UserNotGroupAdministratorAccessDeniedException exception = assertThrows(
                UserNotGroupAdministratorAccessDeniedException.class,
                () -> updateGroupUseCase.execute(request, groupId)
        );

        // Then
        assertEquals("Only group administrators can edit group!", exception.getMessage());
        verify(authApi).getCurrentUser();
        verify(groupRepository).findById(groupId);
        verify(userApi, never()).getUserById(any());
        verify(groupTypeRepository, never()).findById(any());
        verify(groupRepository, never()).save(any());
    }

    @Test
    void shouldThrowUserNotFoundException_whenNewAdminUserDoesNotExist() {
        // Given
        when(authApi.getCurrentUser()).thenReturn(Optional.of(currentUserDto));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(userApi.getUserById(newAdminId)).thenReturn(Optional.empty());

        // When
        UserNotFoundException exception = assertThrows(
                UserNotFoundException.class,
                () -> updateGroupUseCase.execute(request, groupId)
        );

        // Then
        assertEquals("User (admin) with id: " + newAdminId + " not found!", exception.getMessage());
        verify(authApi).getCurrentUser();
        verify(groupRepository).findById(groupId);
        verify(userApi).getUserById(newAdminId);
        verify(groupTypeRepository, never()).findById(any());
        verify(groupRepository, never()).save(any());
    }

    @Test
    void shouldThrowGroupTypeNotFoundException_whenGroupTypeDoesNotExist() {
        // Given
        when(authApi.getCurrentUser()).thenReturn(Optional.of(currentUserDto));
        when(groupRepository.findById(groupId)).thenReturn(Optional.of(group));
        when(userApi.getUserById(newAdminId)).thenReturn(Optional.of(newAdminUser));
        when(groupTypeRepository.findById(GroupTypeEnum.OPEN.getId())).thenReturn(Optional.empty());

        // When
        GroupTypeNotFoundException exception = assertThrows(
                GroupTypeNotFoundException.class,
                () -> updateGroupUseCase.execute(request, groupId)
        );

        // Then
        assertEquals("Group type with id: " + GroupTypeEnum.OPEN.getId() + " not found!", exception.getMessage());
        verify(authApi).getCurrentUser();
        verify(groupRepository).findById(groupId);
        verify(userApi).getUserById(newAdminId);
        verify(groupTypeRepository).findById(GroupTypeEnum.OPEN.getId());
        verify(groupRepository, never()).save(any());
    }
}