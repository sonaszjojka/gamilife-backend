package edu.pjwstk.groupshop.usecase.deletegroupshop;

import edu.pjwstk.common.authApi.AuthApi;
import edu.pjwstk.common.authApi.dto.CurrentUserDto;
import edu.pjwstk.common.groupsApi.GroupApi;
import edu.pjwstk.common.groupsApi.dto.GroupDto;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.groupshop.exception.GroupShopNotFoundException;
import edu.pjwstk.groupshop.exception.UserNotAdministratorException;
import edu.pjwstk.groupshop.repository.GroupShopRepository;
import edu.pjwstk.groupshop.shared.ApiResponse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class DeleteGroupShopUseCaseImpl implements DeleteGroupShopUseCase {

    private final GroupShopRepository groupShopRepository;
    private final GroupApi groupProvider;
    private final AuthApi userProvider;


    public DeleteGroupShopUseCaseImpl(GroupShopRepository groupShopRepository, GroupApi groupProvider, AuthApi userProvider) {
        this.groupShopRepository = groupShopRepository;
        this.groupProvider = groupProvider;
        this.userProvider = userProvider;
    }

    @Override
    public void execute(UUID groupShopId, UUID groupId) {

        groupShopRepository.findByGroupShopId(groupShopId).orElseThrow(
                () -> new GroupShopNotFoundException("Group shop with id: " + groupShopId + " not found!"));

        CurrentUserDto currentUserDto = userProvider.getCurrentUser()
                .orElseThrow();
        GroupDto groupDto = groupProvider.findGroupById(groupId);

        if (!currentUserDto.userId().equals(groupDto.adminId())) {
            throw new UserNotAdministratorException("Only group administrators can delete group shop!");
        }

        groupShopRepository.deleteById(groupShopId);
    }

}
