package edu.pjwstk.groupshop.controllers;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/group-shops")
public class GroupShopController {

    @GetMapping("test")
    public String test() {
        return "Hello World, Group Shop Controller!";
    }

}
