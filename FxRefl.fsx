(* Reflection Utilities for F#

*)

open System
open System.Reflection
// open System.Windows.Forms


// let f = new Form (Text = "Hello world")


//  Type Information
//      
module Tinfo =


    //  Try to find the type by its full name 
    //    
    let tryFindType typeFullName = 

        let asms = AppDomain.CurrentDomain.GetAssemblies ()

        asms
        |> Array.collect (fun (a: Assembly) -> a.GetTypes()  )
        |> Array.tryFind (fun (t: Type) -> t.FullName = typeFullName)


    let getMembers (t: Type) =
        t.GetMembers()

    let showMembers (t: Type) =
        for m in t.GetMembers () do
            Console.WriteLine(m)

    let getProperties (t: Type) =
        t.GetProperties ()

    let showProperties (t: Type) =
        for p in t.GetProperties () do
            Console.WriteLine(p)


    let getFields (t: Type) =
        t.GetFields ()

    let showFields (t: Type) =
        for f in t.GetFields () do
            Console.WriteLine(f)

    let getMethods (t: Type) =
        t.GetMethods (BindingFlags.Public)

    let showMethods (t: Type) =
        for m in t.GetMethods () do
            Console.WriteLine(m)


    let isPublic (t: Type) =
        t.IsPublic

    let isClass (t: Type) =
        t.IsClass    

    let isEnum (t: Type) =
        t.IsEnum

    let isAbstract (t: Type) =
        t.IsAbstract

    let isArray (t: Type) =
        t.IsArray

    let isInterface (t: Type) =
        t.IsInterface

    let isPrimitive (t: Type) =
        t.IsPrimitive

    let isComObject (t: Type) =
        t.IsCOMObject 

    let isSubclassOf cls (t: Type)  =
        t.IsSubclassOf(cls)


module TinfoByName =

    let showMembers tname =
        Tinfo.tryFindType tname
        |> Option.iter Tinfo.showMembers

    let showFields tname =
        Tinfo.tryFindType tname
        |> Option.iter Tinfo.showFields

    let showMethods tname =
        Tinfo.tryFindType tname
        |> Option.iter Tinfo.showMethods


module Inspect = 


    let getClassName objx =
        objx.GetType().Name

    let getClassFullName objx =
        objx.GetType().FullName 

    let getNamespace objx =
        objx.GetType().Namespace

    let getAssemblyName objx =
        objx.GetType().AssemblyQualifiedName


    let private propertyTemplate = "
    Name = {0}
    Type = {1}
    Writable = {2}
     ---------------- 
    "


    let showProperties objx =

        for p in objx.GetType().GetProperties () do

            Console.WriteLine(propertyTemplate,
                              p.Name,
                              p.PropertyType.Name,
                              p.CanWrite )


    let showPropertiesWritable objx =

        for p in objx.GetType().GetProperties () do

            if p.CanWrite
            then (
                Console.WriteLine(propertyTemplate,
                              p.Name,
                              p.PropertyType.Name,
                              p.CanWrite )
            )
            else ()

    let getPropertyValue objx pname =
        objx.GetType().GetProperty(pname).GetValue (objx, null)

    let getPropertyValueAsString objx pname =
            let p = objx.GetType().GetProperty(pname).GetValue(objx, null)
            p.ToString()


    let getPropertyType objx pname =
        objx.GetType().GetProperty(pname).PropertyType

    let getPropertyTypeName objx pname =
          objx.GetType().GetProperty(pname).PropertyType.Name 

    let getProperty objx pname =
        objx.GetType().GetProperty(pname)

    // Set object property by Reflection.  
    //    
    let setPropertyValue objx pname value =
        let prop = objx.GetType().GetProperty(pname,  BindingFlags.Public ||| BindingFlags.Instance)

        if prop <> null && prop.CanWrite
        then prop.SetValue(objx, value, null)
        else ()


    // Set object property by Reflection parsing the string
    //
    let setPropertyValueStr objx pname valueStr =

        let prop = objx.GetType().GetProperty(pname,  BindingFlags.Public ||| BindingFlags.Instance)

        if prop <> null && prop.CanWrite
        then prop.SetValue(objx, Convert.ChangeType(valueStr, prop.PropertyType), null)
        else ()


    // Get all types that belongs to Namespace. 
    let getNamespaceTypes ns = 

        let asms = AppDomain.CurrentDomain.GetAssemblies ()

        asms
        |> Array.collect (fun (a: Assembly) ->
                          a.GetTypes()
                          |> Array.filter (fun (t: Type) -> t.Namespace = ns && t.IsPublic))


    let showNamespaceTypes ns =
        getNamespaceTypes ns 
        |> Array.iter (fun (t: Type) -> Console.WriteLine(t))



// ---------------- Form ---------------------- // $fsi_0499

// Type predicates
//     


module Asm =

    let load fpath =
        Assembly.LoadFile(fpath)


    let loadReflection fpath =
        Assembly.ReflectionOnlyLoadFrom(fpath)

    let getReferecnendAsms (asm: Assembly) =
        asm.GetReferencedAssemblies()
    
    let showPublicClasses (asm: Assembly) =
        for t in asm.GetTypes () do
            if t.IsPublic && t.IsClass
            then Console.WriteLine(t)
            else ()
    
    let getNamespace (asm: Assembly) =
        asm.GetName().Name        
    
    let getType (asm: Assembly) typeNameFull =
        asm.GetType(typeNameFull)

    let getTypeRel (asm: Assembly) typeName =
        let nspace = getNamespace asm
        asm.GetType(nspace + "." + typeName)

    let createInstance (asm: Assembly) typeName =
        Activator.CreateInstance(getTypeRel asm typeName)
       
    let showChildClasses (asm: Assembly) className =
        let cls = getType asm className

        for c in asm.GetTypes() do
            if c.IsSubclassOf(cls)
            then Console.WriteLine(c)
            else ()

    let filterTypes (asm: Assembly) predicate =
        Array.filter predicate (asm.GetTypes())

    let iterTypes (asm: Assembly) predicate action =
        for t in (asm.GetTypes ()) do
            if (predicate t)
            then action t
            else ()

    ///  Get assembly title metadata 
    /// 
    let getAsmtitle  (asm: Assembly) =
        (Attribute.GetCustomAttribute(asm, typeof<AssemblyTitleAttribute>, false) :?>
         AssemblyTitleAttribute).Title

    let getAsmDescription (asm: Assembly) =
        let attr = Attribute.GetCustomAttribute(asm, typeof<AssemblyDescriptionAttribute>, false)
        (attr :?> AssemblyDescriptionAttribute).Description 

    let getAsmCompany (asm: Assembly) =
        let attr = Attribute.GetCustomAttribute(asm, typeof<AssemblyCompanyAttribute>, false)
        (attr :?> AssemblyCompanyAttribute).Company  

    let getAsmConfiguration (asm: Assembly) =
        let attr = Attribute.GetCustomAttribute(asm, typeof<AssemblyConfigurationAttribute>, false)
        (attr :?> AssemblyConfigurationAttribute).Configuration  

    let getAsmProduct (asm: Assembly) =
        let attr =  Attribute.GetCustomAttribute(asm, typeof<AssemblyProductAttribute>, false)
        (attr :?> AssemblyProductAttribute).Product

    let getAsmCopyright (asm: Assembly) =
         let attr =  Attribute.GetCustomAttribute(asm, typeof<AssemblyCopyrightAttribute>, false)
         (attr :?> AssemblyCopyrightAttribute).Copyright

    let getAsmTrademark (asm: Assembly) =
         let attr =  Attribute.GetCustomAttribute(asm, typeof<AssemblyTrademarkAttribute>, false)
         (attr :?> AssemblyTrademarkAttribute).Trademark    

    //  @FIXME: System.NullReferenceException: Object ... 
    // 
    let getAsmCulture (asm: Assembly) =
         let attr =  Attribute.GetCustomAttribute(asm, typeof<AssemblyCultureAttribute>, false)
         (attr :?> AssemblyCultureAttribute).Culture    

    //  @FIXME: Fails: System.NullReferenceException: Object reference not set to an instance of an object
    //     
    let getAsmVersion (asm: Assembly) =
         let attr =  Attribute.GetCustomAttribute(asm, typeof<AssemblyVersionAttribute>, false)
         (attr :?> AssemblyVersionAttribute).Version    






// let form = Activator.CreateInstance(Asm.getTypeRel asm "Form")
        
        //[| for c in asm.GetTypes() -> if predicate c then c |]

// let asm = Assembly.LoadFrom("/usr/lib/mono/4.5-api/System.Windows.Forms.dll")    

// let x = 1 + 10 






