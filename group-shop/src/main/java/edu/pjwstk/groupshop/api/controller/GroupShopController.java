package edu.pjwstk.groupshop.api.controller;

import edu.pjwstk.groupshop.shared.ApiResponse;

import edu.pjwstk.groupshop.usecase.changeGroupShopStatus.ChangeGroupShopStatusRequest;
import edu.pjwstk.groupshop.usecase.changeGroupShopStatus.ChangeGroupShopStatusResponse;
import edu.pjwstk.groupshop.usecase.changeGroupShopStatus.ChangeGroupShopStatusUseCase;
import edu.pjwstk.groupshop.usecase.editgroupshop.EditGroupShopRequest;
import edu.pjwstk.groupshop.usecase.editgroupshop.EditGroupShopResponse;
import edu.pjwstk.groupshop.usecase.editgroupshop.EditGroupShopUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/shop")
public class GroupShopController {
    private final ChangeGroupShopStatusUseCase changeGroupShopStatusUseCase;
    private final EditGroupShopUseCase editGroupShopUseCase;


    public GroupShopController(ChangeGroupShopStatusUseCase changeGroupShopStatusUseCase, EditGroupShopUseCase editGroupShopUseCase) {
        this.changeGroupShopStatusUseCase = changeGroupShopStatusUseCase;
        this.editGroupShopUseCase = editGroupShopUseCase;
    }



    @PutMapping("/{shopId}")
    public ResponseEntity<EditGroupShopResponse> editGroupShop(@PathVariable (name = "groupId") UUID groupId ,
                                                               @PathVariable(name="shopId") UUID shopId,
                                                               @RequestBody EditGroupShopRequest request) {

        EditGroupShopResponse response = editGroupShopUseCase.execute(request, shopId, groupId);
        return ResponseEntity.ok(response);
    }

    @PutMapping("/{shopId}/status")
    public ResponseEntity<ChangeGroupShopStatusResponse> changeGroupShopStatus(@PathVariable(name = "groupId") UUID groupId,
                                                             @PathVariable(name = "shopId") UUID shopId,
                                                             @RequestBody@Valid ChangeGroupShopStatusRequest request) {
        ChangeGroupShopStatusResponse response = changeGroupShopStatusUseCase.execute(request,shopId, groupId);
        return ResponseEntity.ok(response);
    }
}
