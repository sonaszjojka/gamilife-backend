package edu.pjwstk.groupshop.controller;

import edu.pjwstk.groupshop.shared.ApiResponse;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopRequest;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopResponse;
import edu.pjwstk.groupshop.usecase.creategroupshop.CreateGroupShopUseCase;
import edu.pjwstk.groupshop.usecase.deletegroupshop.DeleteGroupShopUseCase;
import edu.pjwstk.groupshop.usecase.editgroupshop.EditGroupShopRequest;
import edu.pjwstk.groupshop.usecase.editgroupshop.EditGroupShopResponse;
import edu.pjwstk.groupshop.usecase.editgroupshop.EditGroupShopUseCase;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop")
public class GroupShopController {
    private final CreateGroupShopUseCase createGroupUseCase;
    private final DeleteGroupShopUseCase deleteGroupShopUseCase;
    private final EditGroupShopUseCase editGroupShopUseCase;


    public GroupShopController(CreateGroupShopUseCase createGroupUseCase, DeleteGroupShopUseCase deleteGroupShopUseCase, EditGroupShopUseCase editGroupShopUseCase) {
        this.createGroupUseCase = createGroupUseCase;
        this.deleteGroupShopUseCase = deleteGroupShopUseCase;
        this.editGroupShopUseCase = editGroupShopUseCase;
    }

    @PostMapping()
    public ResponseEntity<CreateGroupShopResponse> createGroupShop(@PathVariable(name="groupId") UUID groupId,
                                                                   @RequestBody CreateGroupShopRequest request) {
       CreateGroupShopResponse response= createGroupUseCase.execute(request, groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{shopId}")
    public ResponseEntity<ApiResponse> deleteGroupShop(@PathVariable(name="shopId") UUID shopId,
                                                       @PathVariable(name="groupId") UUID groupId) {


        deleteGroupShopUseCase.execute(shopId, groupId);
        return ResponseEntity.ok(new ApiResponse("Group shop with id: " + shopId + " deleted successfully."));
    }

    @PutMapping("/{shopId}")
    public ResponseEntity<EditGroupShopResponse> editGroupShop(@PathVariable (name = "groupId") UUID groupId ,
                                                               @PathVariable(name="shopId") UUID shopId,
                                                               @RequestBody EditGroupShopRequest request) {

        EditGroupShopResponse response = editGroupShopUseCase.execute(request, shopId, groupId);
        return ResponseEntity.ok(response);
    }
}
